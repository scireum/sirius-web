package sirius.web.service;

import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.xml.StructuredOutput;
import sirius.web.services.ServiceCall;
import sirius.web.services.StructuredService;

@Register(name = "test/default-error")
public class DefaultErrorService implements StructuredService {
    @Override
    public void call(ServiceCall call, StructuredOutput out) throws Exception {
        throw Exceptions.createHandled().withSystemErrorMessage("error error error").handle();
    }
}
