package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;
import java.util.Stack;

public class ThrowCommand implements Command {
    @Override
    public Object execute(Stack<MethodCallEntry> stack, Object calledObject, String[] args) {
        return calledObject;
    }
}
