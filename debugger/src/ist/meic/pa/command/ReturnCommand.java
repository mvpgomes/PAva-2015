package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;
import ist.meic.pa.Tuple;

import java.util.Stack;

public class ReturnCommand extends Command {
    @Override
    public Tuple<Boolean, Object> execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) {
        MethodCallEntry calledMethod = stack.peek();
        Class returnTypeClass = calledMethod.getResultSig();

        Object res = getParameterParser(returnTypeClass.getSimpleName()).parse(args[0]);
        return new Tuple<>(Boolean.TRUE, res);
    }
}

