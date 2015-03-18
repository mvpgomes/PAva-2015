package ist.meic.pa;

public class CallInstance {
    private final Object instance;
    private final String methodName;
    private final Object[] methodArgs;

    public CallInstance(Object instance, String methodName, Object... methodArgs) {
        this.instance = instance;
        this.methodName = methodName;
        this.methodArgs = methodArgs;
    }

    public Object getInstance() {
        return instance;
    }

    public String getMethodName() {
        return methodName;
    }

    public Object[] getMethodArgs() {
        return methodArgs;
    }
}
