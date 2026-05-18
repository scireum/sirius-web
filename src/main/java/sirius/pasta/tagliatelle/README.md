# Tagliatelle Templating Mechanisms

This package implements Sirius' statically compiled `*.pasta` template engine for HTML, XML, PDF, JS, CSS, and text
output. At a high level, Tagliatelle parses a template into emitters, compiles embedded Noodle expressions, and renders
the result with a shared global output buffer plus per-template local contexts.

## Mental Model

Tagliatelle templates mix three things:

1. Static text
2. Noodle expressions and macro calls
3. XML-like tags, either built-in `i:*` tags or user-facing taglibs like `t:*`

During compilation, `TemplateCompiler` walks the input, turns it into an emitter tree, resolves referenced templates and
taglibs, and records template arguments and pragmas. During rendering, a `GlobalRenderContext` owns the output buffer,
escaping mode, extra blocks, guards, and template cache, while each invocation receives its own `LocalRenderContext`
with local variables and block bindings.

## Files And Resolution

- Regular templates are resolved by an absolute resource path such as `/templates/http/error.html.pasta`.
- Only files ending in `.pasta` are accepted as templates.
- Taglib tags are backed by templates in `/taglib/<prefix>/<name>.html.pasta`.
    - Example: `/taglib/t/heading.html.pasta` is invoked as `<t:heading ...>`.
- Extension templates live below `/extensions/<target>/...` or `/default/extensions/<target>/...` and are rendered via
  `i:extensions`.

## Expressions, Function Calls, And Escaping

### `@...` output expressions

`@expression` evaluates a Noodle expression and writes the result using the active escaper.

Examples:

```pasta
<div>@user.getName()</div>
<span>@i18n("app.title")</span>
```

For XML-like output (`.html`, `.xml`, `.pdf`), Tagliatelle automatically escapes string output as XML. For non-XML-like
output, raw output is the default.

### `___...` inline expressions

`___expression` is the same mechanism as `@expression`, but is useful inside JavaScript or CSS where a literal `@` would
be awkward or ambiguous.

Examples from the codebase include:

```pasta
const message = '___i18n("template.html.uploadFailed")';
___include("/assets/tycho/libs/bootstrap/js/bootstrap.min.js")
```

### Escaping the expression markers

- `@@` renders a literal `@`
- `@___` renders a literal `___`

### Raw output

Use `i:raw` or `@raw() { ... }` to temporarily disable escaping and emit the inner result unchanged. This is intended
for trusted HTML or pre-rendered fragments.

```pasta
<i:raw>@message.getHtml()</i:raw>
```

## Comments

Tagliatelle strips two template-comment forms during compilation:

- HTML-style: `<!--@ ... -->`
- JS/CSS-style: `/**@ ... */`

These are true template comments, not runtime output comments, so they never reach the rendered result.

## Macros And Noodle Calls

Expressions are compiled by the Noodle compiler, so template code can call regular Noodle macros and methods in addition
to the Tagliatelle-specific helpers in `macros/`.

Examples:

```pasta
<i:local name="actions" value="@renderToString('body')"/>
<i:arg type="String" name="id" default="@generateId('widget-%s')"/>
<i:if test="ifNotDefined('page-scripts')">
    ...
</i:if>
```

Registered macros can be checked via the `/system/tags` route.

## Built-In `i:*` Tags

The internal tag family is resolved by `TemplateCompilationContext.findTagHandler(...)`. These tags are compiler
features, not regular taglib templates.

### Signature And Metadata

- `i:arg`: declares a template argument, including type, default value, description, and deprecation warning
- `i:pragma`: attaches metadata to the current template
    - Common pragmas in this package are `description`, `deprecated`, and `alias`

Typical header in a taglib template:

```pasta
<i:arg type="String" name="label"/>
<i:pragma name="description" value="Renders a heading"/>
```

### Template Invocation

- `i:invoke`: invokes another template by constant path and maps tag attributes to template arguments
- `i:dynamicInvoke`: invokes a template selected by runtime expression
- `i:include`: includes an asset resource as literal content without further template processing

There are also expression forms:

- `@invoke("/path/to/template.pasta")`
- `@include("/assets/...")`

Use `invoke` when the target is another Tagliatelle template and should be compiled and rendered. Use `include` when the
target is a static asset and should be copied literally.

## Variables, Flow Control, And Blocks

### Variables

- `i:local`: stores an expression result in a local variable
- `i:define`: renders its body to a string and stores it in a local variable

### Flow Control

- `i:if` / `i:else`: conditional rendering
- `i:for`: iterates over an iterable or array and exposes a loop variable plus optional `LoopState`
- `i:switch`: selects an inner `i:block` by block name matching the switch expression

### Blocks And Slots

- `i:block`: declares a named block
    - inside an invocation, it passes a block to the callee
    - at the top level, it creates an extra block in the global render context
- `i:render`: renders a provided block, optionally with fallback body content
- `i:extraBlock`: writes a named extra block into the global render context from nested templates
- `i:extensions`: renders all extension templates registered for a target

This is the mechanism behind slot-like APIs in taglibs:

```pasta
<t:heading label="Users">
    <i:block name="actions">
        <button>Refresh</button>
    </i:block>
</t:heading>
```

The callee can later emit the provided block via:

```pasta
<i:render name="actions"/>
```

## Taglibs

User-facing reusable components are plain templates stored in `/taglib/<prefix>/<name>.html.pasta`. When the compiler
sees `<prefix:name>`, it resolves the matching template and wraps it in a `TaglibTagHandler`.

Important implications:

- taglib attributes are validated against the callee template's `i:arg` declarations
- required arguments are inferred from missing defaults
- the default unnamed body is passed as block `body`
- additional named blocks are passed via inner `i:block` tags
- taglib templates can themselves use `i:*` tags, macros, and invoke other templates

## Pragmas

Pragmas are compile-time metadata stored on `Template`. They are not rendered directly but are consumed by tooling and
the engine.

Notable behaviors in this package:

- `description`: used by `/system/tags` and `/system/tags.md` to describe taglibs
- `deprecated`: triggers warnings when deprecated templates or arguments are referenced
- `alias`: makes a template resolve to another template path or tag name

## Sandbox And Security

Tagliatelle sits on top of Noodle, so sandboxing comes from the Noodle compilation layer.

- Resource-backed application templates are compiled normally.
- Inline templates can be compiled with an explicit `SandboxMode`.
- When sandboxing is active, method calls, constructor calls, field access, and macro usage are checked at compile time.
- Macros or helper types explicitly annotated with `@NoodleSandbox(GRANTED)` remain callable in sandboxed code.

Security-related restrictions that are visible directly in Tagliatelle include:

- only `.pasta` files can be resolved as templates
- `include` only allows assets, not arbitrary resources
- recursive template compilation is detected and rejected

## Discovery And Tooling

Two built-in routes are especially useful when exploring available syntax:

- `/system/tags`: HTML overview of macros, built-in tags, and taglibs
- `/system/tags.md`: markdown inventory generated from the live system state

## Quick Example

```pasta
<i:arg type="String" name="title"/>
<i:arg type="String" name="id" default="@generateId('panel-%s')"/>
<i:pragma name="description" value="Simple example component"/>

<div class="panel" id="@id">
    <h2>@title</h2>

    <i:if test="ifNotDefined('panel-style')">
        <i:extraBlock name="styles">
            <style>.panel { padding: 1rem; }</style>
        </i:extraBlock>
    </i:if>

    <i:render name="body"/>
</div>
```

This example shows the typical Tagliatelle building blocks together: declared arguments, macro-backed defaults,
escaping, a guard, an extra block, and slot rendering.
