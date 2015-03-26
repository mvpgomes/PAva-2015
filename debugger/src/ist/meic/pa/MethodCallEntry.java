package ist.meic.pa;

public class MethodCallEntry {
    private final Class instanceClass;
    private final Object instance;
    private final String methodName;
    private final Class[] methodArgsSig;
    private final Object[] methodArgs;
    private final Class resultSig;

    public MethodCallEntry(Class instanceClass, Object instance, String methodName, Class[] methodArgsSig, Object[] methodArgs,
                           Class resultSig) {
        this.instanceClass = instanceClass;
        this.instance = instance;
        this.methodName = methodName;
        this.methodArgsSig = methodArgsSig;
        this.methodArgs = methodArgs;
        this.resultSig = resultSig;
    }

    public Class getInstanceClass() {
        return instanceClass;
    }

    public Object getInstance() {
        return instance;
    }

    public String getMethodName() {
        return methodName;
    }

    public Class[] getMethodArgsSig() {
        return methodArgsSig;
    }

    public Object[] getMethodArgs() {
        return methodArgs;
    }

    public Class getResultSig() {
        return resultSig;
    }

    private static String printArgArray(Object[] args) {
        String delim = "";
        StringBuilder sb = new StringBuilder();
        for (Object o : args) {
            sb.append(delim);
            sb.append(o.getClass().isArray() ? printArgArray((Object[]) o) : o.toString());
            delim = ",";
        }
        return sb.toString();
    }

    public String print() {
        return this.instanceClass.getCanonicalName() + "." + this.methodName + "(" + printArgArray(this.methodArgs) + ")";
    }
}
