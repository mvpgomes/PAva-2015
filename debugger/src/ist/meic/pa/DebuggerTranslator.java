package ist.meic.pa;

import javassist.*;
import javassist.expr.ExprEditor;
import javassist.expr.MethodCall;

public class DebuggerTranslator implements Translator {
    private final String packageName;

    public DebuggerTranslator(String packageName) {
        this.packageName = packageName;
    }

    private static void instrumentMethods(final CtClass c) {
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
                if (!m.isEmpty()) {
                    m.instrument(
                            new ExprEditor() {
                                public void edit(MethodCall mc) throws CannotCompileException {
                                    try {
                                        if (mc.getMethod().getParameterTypes().length == 0) {
                                            mc.replace(String.format("{ $_ = ($r)ist.meic.pa.Debugger.getInstance().proxy($0, $class, \"%s\", $sig, $type); }", mc.getMethodName()));
                                        } else {
                                            mc.replace(String.format("{ $_ = ($r)ist.meic.pa.Debugger.getInstance().proxy($0, $class, \"%s\", $sig, $type, $$); }", mc.getMethodName()));
                                        }
                                    } catch (Throwable t) {
                                        throw new RuntimeException(t);
                                    }
                                }
                            }
                    );
                }
            }
        } catch (Throwable t) {
            // should not happen
            throw new RuntimeException(t);
        }
    }

    // TODO format string with String.format
    private static void injectAddCallStack(CtMethod m) {
        try {
            final String instanceCmd = Modifier.isStatic(m.getModifiers()) ? "null" : "$0";
            final boolean isVoid = m.getReturnType().getSimpleName().equals("void");
            final String returnCmd = isVoid ? "null" : "$type";
            m.insertBefore(String.format("{ ist.meic.pa.Debugger.getInstance().addCall($class, %s, \"%s\", $sig, $args, %s); }",
                    instanceCmd, m.getName(), returnCmd));
        } catch (Throwable t) {
            throw new RuntimeException(t);
        }
    }

    private static void injectInspect(CtMethod m) {
        try {
            final String returnType = m.getReturnType().getSimpleName();
            final CtClass etype = ClassPool.getDefault().get("java.lang.Throwable");



            // Note that CtMethod.addCatch must throw exception or return value.
            switch (returnType) {
                case "void":
                    m.addCatch("{ ist.meic.pa.Debugger.getInstance().inspect($e); return; }", etype);
                    break;
                case "short":
                    m.addCatch("{ return ((Short)ist.meic.pa.Debugger.getInstance().inspect($e)).shortValue(); }", etype);
                    break;
                case "int":
                    m.addCatch("{ return ((Integer)ist.meic.pa.Debugger.getInstance().inspect($e)).intValue(); }", etype);
                    break;
                case "long":
                    m.addCatch("{ return ((Long)ist.meic.pa.Debugger.getInstance().inspect($e)).longValue(); }", etype);
                    break;
                case "double":
                    m.addCatch("{ return ((Double)ist.meic.pa.Debugger.getInstance().inspect($e)).doubleValue(); }", etype);
                    break;
                case "float":
                    m.addCatch("{ return ((Float)ist.meic.pa.Debugger.getInstance().inspect($e)).floatValue(); }", etype);
                    break;
                case "boolean":
                    m.addCatch("{ return ((Boolean)ist.meic.pa.Debugger.getInstance().inspect($e)).booleanValue(); }", etype);
                    break;
                case "byte":
                    m.addCatch("{ return ((Byte)ist.meic.pa.Debugger.getInstance().inspect($e)).byteValue(); }", etype);
                    break;
                case "char":
                    m.addCatch("{ return ((Character)ist.meic.pa.Debugger.getInstance().inspect($e)).charValue(); }", etype);
                    break;
                default:
                    // TODO support objects
                    throw new RuntimeException("There is no support for that type.");
            }
        } catch (Throwable t) {
            throw new RuntimeException(t);
        }
    }

    @Override
    public void start(ClassPool classPool) throws NotFoundException, CannotCompileException {
        // Empty on purpose
    }

    @Override
    public void onLoad(ClassPool classPool, String className) throws NotFoundException, CannotCompileException {
        if (className.startsWith(packageName)) {
            CtClass c = classPool.get(className);
            instrumentMethods(c);
        }
    }
}
