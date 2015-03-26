package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;

import java.lang.reflect.Field;
import java.util.Stack;

public class InfoCommand extends Command {
    @Override
    public Object execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) {
        final MethodCallEntry e = stack.peek();
        final Object instance = e.getInstance();

        // Print the instance
        System.out.println(String.format("Called Object: %s", instance == null ? "null" : instance.toString()));

        Class instanceClass = e.getInstanceClass();

        // Print the available fields of the instance class
        System.out.print("       Fields:");
        Field[] fields = instanceClass.getDeclaredFields();
        for (Field f : fields) {
            System.out.print(" " + f.getName());
        }
        System.out.println();

        // Print the current call stack
        System.out.println("Call stack:");
        for (int i = stack.size() - 1; i >= 0; i--) {
            System.out.println(stack.elementAt(i).toString());
        }
        return null;
    }
}

