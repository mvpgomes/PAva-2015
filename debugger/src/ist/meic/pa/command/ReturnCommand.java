package ist.meic.pa.command;

import ist.meic.pa.Debugger;
import ist.meic.pa.GenericParser;
import ist.meic.pa.MethodCallEntry;
import ist.meic.pa.Tuple;

import java.util.Stack;

public class ReturnCommand implements Command {
    @Override
    public Tuple<Boolean, Object> execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) {
        final MethodCallEntry calledMethod = stack.peek();
        Debugger.getInstance().removeLastCall();

        final Class returnTypeClass = calledMethod.getResultSig();
        final Object res = GenericParser.parse(returnTypeClass, args[0]);
        return new Tuple<>(Boolean.TRUE, res);
    }
}

