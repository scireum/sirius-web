package sirius.web.controller;

import sirius.kernel.di.std.Part;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;
import sirius.web.sass.Generator;

import java.io.IOException;
import java.io.InputStream;
import java.util.Optional;

class TestGenerator extends Generator {
    @Override
    public void debug(String message) {

    }

    @Override
    public void warn(String message) {
        throw new RuntimeException(message);
    }

    @Part
    private static Resources resources;

    @Override
    protected InputStream resolveIntoStream(String sheet) throws IOException {
        Optional<Resource> res = resources.resolve(sheet);
        if (res.isPresent()) {
            return res.get().getUrl().openStream();
        }
        return null;
    }

    TestGenerator() {
    }
}
