package ist.meic.pa;

public class CallInstance {
    private final Class instanceClass;
    private final Object instance;
    private final String methodName;
    private final Class[] methodSig;
    private final Object[] methodArgs;
    private final Class resultSig;

    public CallInstance(Class instanceClass, Object instance, String methodName, Class[] methodSig, Object[] methodArgs,
                        Class resultSig) {
        this.instanceClass = instanceClass;
        this.instance = instance;
        this.methodName = methodName;
        this.methodSig = methodSig;
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

    public Class[] getMethodSig() {
        return methodSig;
    }

    public Object[] getMethodArgs() {
        return methodArgs;
    }

    public Class getResultSig() {
        return resultSig;
    }
}
