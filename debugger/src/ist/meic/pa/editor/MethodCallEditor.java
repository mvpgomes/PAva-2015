package ist.meic.pa.editor;

import javassist.CannotCompileException;
import javassist.expr.ExprEditor;
import javassist.expr.MethodCall;

/**
 * This implementation replaces all the method calls that it finds with an adequate proxy method available in the
 * Debugger class.
 */
public class MethodCallEditor extends ExprEditor {
    @Override
    public void edit(MethodCall m) throws CannotCompileException {
        m.replace(String.format("{ $_ = ($r) ist.meic.pa.Debugger.getInstance().proxyMethod($class, $0, \"%s\", $sig, $args, $type); }", m.getMethodName()));
    }
}
