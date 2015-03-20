package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;
import java.util.Stack;

public class InfoCommand implements Command {
    @Override
    public void execute(Stack<MethodCallEntry> stack, Object calledObject, String[] args) {
        System.out.print("Called Object:");
        // Need to print the called object
        System.out.println(calledObject.toString());
        System.out.println();
        // Need to print the stack
        System.out.println("Call stack:");
        for(MethodCallEntry entry : stack) {
            entry.toString();
        }
    }
}

