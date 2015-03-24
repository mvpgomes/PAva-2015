package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;

import java.lang.reflect.Method;
import java.util.Stack;

public class RetryCommand extends Command {
    @Override
    public Object execute(Stack<MethodCallEntry> stack, Object calledObject, String[] args, Throwable t) throws Throwable {
        MethodCallEntry calledMethod = stack.pop();
        String methodName = calledMethod.getMethodName();
        Class[] argsClass = calledMethod.getMethodSig();
        Object[] methodArgs = calledMethod.getMethodArgs();
        Method m  = calledObject.getClass().getDeclaredMethod(methodName, argsClass);
        m.invoke(calledObject, methodArgs);
        return null;
    }
}
