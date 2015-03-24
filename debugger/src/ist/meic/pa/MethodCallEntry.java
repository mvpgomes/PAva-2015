package ist.meic.pa;

public class MethodCallEntry {
    private final Class instanceClass;
    private final Object instance;
    private final String methodName;
    private final Class[] methodSig;
    private final Object[] methodArgs;
    private final Class resultSig;

    public MethodCallEntry(Class instanceClass, Object instance, String methodName, Class[] methodSig, Object[] methodArgs,
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

    public String printArgs(Object[] methodArgs){
        String args = "(";
        for(int i=0; i < methodArgs.length - 1; i++){
            args = methodArgs[i].toString() + ",";
        }
        return args + methodArgs[methodArgs.length - 1] + ")";
    }

    @Override
    public String toString(){
        return this.instanceClass.getName() + "." + this.methodName + this.printArgs(this.methodArgs);
    }
}
