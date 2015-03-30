package ist.meic.pa.command;

import ist.meic.pa.Debugger;
import ist.meic.pa.MethodCallEntry;
import ist.meic.pa.Tuple;

import java.util.Stack;

/**
 * The retry command invokes the last method in the method call stack (the method that generated the exception that led
 * to the repl).
 */
public class RetryCommand implements Command {
    /**
     * Note that this implementation removes the last element from the stack.
     */
    @Override
    @SuppressWarnings("unchecked")
    public Tuple<Boolean, Object> execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) throws Throwable {
        final MethodCallEntry calledMethod = stack.pop();

        Class instanceClass = calledMethod.getInstanceClass();
        Object instance = calledMethod.getInstance();

        String methodName = calledMethod.getMethodName();
        Class[] methodArgsSig = calledMethod.getMethodArgsSig();
        Object[] methodArgs = calledMethod.getMethodArgs();
        Class resultSig = calledMethod.getResultSig();

        Object res = Debugger.getInstance().callProxyMethod(instanceClass, instance, methodName, methodArgsSig, methodArgs, resultSig);

        return new Tuple<>(Boolean.TRUE, res);
    }
}
