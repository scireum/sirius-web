package sirius.web.service;

import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.xml.StructuredOutput;
import sirius.web.services.ServiceCall;
import sirius.web.services.StructuredService;

@Register(name = "test/special-error")
public class SpecialErrorService implements StructuredService {

    @Override
    public void call(ServiceCall call, StructuredOutput out) throws Exception {
        throw Exceptions.createHandled().withSystemErrorMessage("error error error").handle();
    }

    @Override
    public void handleException(ServiceCall call, StructuredOutput out, HandledException e) {
        out.beginResult();
        out.property("type", "special error service");
        out.property("message", e.getMessage());
        out.endResult();
    }
}
