package ist.meic.pa;

import ist.meic.pa.editor.ConstructorCallEditor;
import ist.meic.pa.editor.MethodCallEditor;
import javassist.*;
import javassist.compiler.ast.Expr;
import javassist.expr.ExprEditor;

import java.util.List;

/**
 * This class is used to instrument all the methods of the to be debugged app.
 * It does not instrument classes from javassist or from this project.
 */
public class ClassEditor implements Translator {
    private List<ExprEditor> editors;

    public ClassEditor(List<ExprEditor> editors) {
        this.editors = editors;
    }

    private void instrumentMethods(final CtClass c) {
        try {
            for (CtMethod cm : c.getDeclaredMethods()) {
                if (!cm.isEmpty()) {
                    for (ExprEditor editor : editors) {
                        cm.instrument(editor);
                    }
                }
            }
        } catch (Throwable t) {
            // should not happen
            throw new RuntimeException(t);
        }

        try {
            for (CtConstructor cc : c.getDeclaredConstructors()) {
                if (!cc.isEmpty()) {
                    for (ExprEditor editor : editors) {
                        cc.instrument(editor);
                    }
                }
            }
        } catch (Throwable t) {
            // should not happen
            throw new RuntimeException(t);
        }
    }

    @Override
    public void start(ClassPool classPool) throws NotFoundException, CannotCompileException {
        // Empty on purpose
    }

    @Override
    public void onLoad(ClassPool classPool, String className) throws NotFoundException, CannotCompileException {
        if (!className.startsWith("ist.meic.pa") && !className.startsWith("javassist")) {
            CtClass c = classPool.get(className);
            instrumentMethods(c);
        }
    }
}
