package ist.meic.pa.command;

import ist.meic.pa.GenericParser;
import ist.meic.pa.MethodCallEntry;
import ist.meic.pa.Tuple;

import java.util.Stack;

/**
 * The return command returns from the last method in the call stack with a given value.
 * This command expects an argument, args[0], to be the value to be returned.
 */
public class ReturnCommand implements Command {
    /**
     * Note that this implementation removes the last element from the stack.
     */
    @Override
    public Tuple<Boolean, Object> execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) {
        final MethodCallEntry calledMethod = stack.pop();
        final Class returnTypeClass = calledMethod.getResultSig();
        final Object res = GenericParser.parse(returnTypeClass, args[0]);
        return new Tuple<>(Boolean.TRUE, res);
    }
}

