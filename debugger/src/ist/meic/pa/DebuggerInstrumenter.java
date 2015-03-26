package ist.meic.pa;

import javassist.*;
import javassist.expr.ConstructorCall;
import javassist.expr.ExprEditor;
import javassist.expr.MethodCall;

import java.util.HashSet;
import java.util.Set;

public class DebuggerInstrumenter extends ExprEditor implements Translator {
    private Set<String> instrumentedMethodNames;

    public DebuggerInstrumenter() {
        this.instrumentedMethodNames = new HashSet<>();
    }

    private void instrumentMethods(final CtClass c) {
        if (c == null || c.getName().equals("java.lang.Object")) {
            return;
        }

        // instrument parent
        try {
            instrumentMethods(c.getSuperclass());
        } catch (NotFoundException e) {
            // should not happen
            throw new RuntimeException(e);
        }

        try {
            for (CtMethod m : c.getDeclaredMethods()) {
//                System.out.println("Instrumenting method calls inside " + m.getLongName());
                if (!m.isEmpty() && !instrumentedMethodNames.contains(m.getLongName())) {
                    m.instrument(this);
                    instrumentedMethodNames.add(m.getLongName());
                }
            }
        } catch (Throwable t) {
            // should not happen
            throw new RuntimeException(t);
        }

        /*
        try {
            for (CtConstructor cc : c.getDeclaredConstructors()) {
                System.out.println("Instrumenting constructor: " + cc.getName());
                if (!cc.isEmpty()) {
                    cc.instrument(this);
                }
            }
        } catch (Throwable t) {
            // should not happen
            throw new RuntimeException(t);
        }
        */
    }

    @Override
    public void edit(MethodCall m) throws CannotCompileException {
        m.replace(String.format("{ $_ = ($r)ist.meic.pa.Debugger.getInstance().proxy($class, $0, \"%s\", $sig, $args, $type); }", m.getMethodName()));
    }

    @Override
    public void edit(ConstructorCall c) throws CannotCompileException {
        c.replace(String.format("{ ist.meic.pa.Debugger.getInstance().proxy($class, \"%s\", $sig, $args); }", c.getMethodName()));
    }

    @Override
    public void start(ClassPool classPool) throws NotFoundException, CannotCompileException {
        // Empty on purpose
    }

    @Override
    public void onLoad(ClassPool classPool, String className) throws NotFoundException, CannotCompileException {
        if (!className.startsWith("ist.meic.pa") && !className.startsWith("javassist")) {
//            System.out.println("Instrumenting class: " + className);
            CtClass c = classPool.get(className);
            instrumentMethods(c);
//            System.out.println("I've instrumented class: " + className);
        }
    }
}
