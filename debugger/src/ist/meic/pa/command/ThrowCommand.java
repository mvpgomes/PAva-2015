package ist.meic.pa.command;

import ist.meic.pa.Debugger;
import ist.meic.pa.MethodCallEntry;
import java.util.Stack;

public class ThrowCommand implements Command {
    @Override
    public Object execute(Stack<MethodCallEntry> stack, Object calledObject, String[] args) throws Throwable {
        throw Debugger.getThrowedException();
    }
}
