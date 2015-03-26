package ist.meic.pa;

import javassist.*;
import javassist.expr.ExprEditor;
import javassist.expr.MethodCall;
import javassist.expr.NewExpr;

public class ClassEditor extends ExprEditor implements Translator {
    private String testClass;

    public ClassEditor(String testClass) {
        this.testClass = testClass;
    }

    private void instrumentMethods(final CtClass c) {
        try {
            for (CtMethod m : c.getDeclaredMethods()) {
//                System.out.println("Instrumenting method calls inside " + m.getLongName());
                if (!m.isEmpty()) {
                    m.instrument(this);
                }
            }
        } catch (Throwable t) {
            // should not happen
            throw new RuntimeException(t);
        }

        try {
            for (CtConstructor cc : c.getDeclaredConstructors()) {
//                System.out.println("Instrumenting constructor: " + cc.getName());
                if (!cc.isEmpty()) {
                    cc.instrument(this);
                }
            }
        } catch (Throwable t) {
            // should not happen
            throw new RuntimeException(t);
        }
    }

    @Override
    public void edit(MethodCall m) throws CannotCompileException {
        m.replace(String.format("{ $_ = ($r) ist.meic.pa.Debugger.getInstance().proxyMethod($class, $0, \"%s\", $sig, $args, $type); }", m.getMethodName()));
    }

    @Override
    public void edit(NewExpr e) throws CannotCompileException {
        try {
            e.replace(String.format("{ $_ = ($r) ist.meic.pa.Debugger.getInstance().proxyConstructor(\"%s\", $sig, $args, $type); }", e.getConstructor().getName()));
        } catch (NotFoundException nfe) {
            throw new RuntimeException(nfe);
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
//            System.out.println("Instrumenting class: " + className);
            instrumentMethods(c);
//            System.out.println("I've instrumented class: " + className);
        }
    }
}
