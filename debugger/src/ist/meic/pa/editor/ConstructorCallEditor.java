package ist.meic.pa.editor;

import javassist.CannotCompileException;
import javassist.NotFoundException;
import javassist.expr.ExprEditor;
import javassist.expr.NewExpr;

/**
 * This implementation replaces all the constructor calls that it finds with an adequate proxy method available in the
 * Debugger class.
 */
public class ConstructorCallEditor extends ExprEditor {
    @Override
    public void edit(NewExpr e) throws CannotCompileException {
        try {
            e.replace(String.format("{ $_ = ($r) ist.meic.pa.Debugger.getInstance().proxyConstructor(\"%s\", $sig, $args, $type); }", e.getConstructor().getName()));
        } catch (NotFoundException nfe) {
            throw new RuntimeException(nfe);
        }
    }
}
