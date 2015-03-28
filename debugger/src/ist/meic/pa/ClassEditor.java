package ist.meic.pa;

import ist.meic.pa.editor.ConstructorCallEditor;
import ist.meic.pa.editor.MethodCallEditor;
import javassist.*;

/**
 * This class is used to instrument all the methods of the to be debugged app.
 * It does not instrument classes from javassist or from this project.
 */
public class ClassEditor implements Translator {
    private static final MethodCallEditor methodCallEditor = new MethodCallEditor();
    private static final ConstructorCallEditor constructorCallEditor = new ConstructorCallEditor();

    private void instrumentMethods(final CtClass c) {
        try {
            for (CtMethod m : c.getDeclaredMethods()) {
                if (!m.isEmpty()) {
                    m.instrument(methodCallEditor);
                }
            }
        } catch (Throwable t) {
            // should not happen
            throw new RuntimeException(t);
        }

        try {
            for (CtConstructor cc : c.getDeclaredConstructors()) {
                if (!cc.isEmpty()) {
                    cc.instrument(constructorCallEditor);
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
