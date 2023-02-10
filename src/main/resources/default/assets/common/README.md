# Common JavaScript Library

Provides a helper library providing commonly used JS snippets. Use or include
[common.js.pasta](common.js.pasta) which brings all other parts with it.

Alternatively use or include [components.js.pasta](components.js.pasta) and [components.scss](components.scss)
to also get access to the larger components like the [slider](scripts/slider.js.pasta),
[history handling](scripts/continuity.js.pasta) and many more.

Additionally, for IE include [ie-polyfill.js](ie-polyfill.js.pasta) which brings
some libraries otherwise missing from Internet Explorer.

Note that the library is internally split up into several files to maintain some
kind of overview and readability.

Note that all functionality provided by this are wrapped in the **sirius** object
to avoid any namespace clashes.

Note that jQuery should not be used throughout this library as we try keeping it dependency free.
