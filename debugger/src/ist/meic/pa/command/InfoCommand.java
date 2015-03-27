package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;
import ist.meic.pa.Tuple;

import java.lang.reflect.Field;
import java.util.Stack;

/**
 * The info command prints the instance in the top of the call stack and its corresponding field, and also prints
 * information about the methods in the call stack, such as method name, method args and corresponding values.
 * This method does not expect any arguments.
 */
public class InfoCommand implements Command {
    @Override
    public Tuple<Boolean, Object> execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) {
        final MethodCallEntry e = stack.peek();
        final Object instance = e.getInstance();

        // Print the instance
        System.out.println(String.format("Called Object: %s", instance == null ? "null" : instance.toString()));

        Class instanceClass = e.getInstanceClass();

        // Print the available fields of the instance class
        System.out.print("       Fields:");
        Field[] fields = instanceClass.getDeclaredFields();
        for (Field f : fields) {
            f.setAccessible(true);
            System.out.print(" " + f.getName());
        }
        System.out.println();

        // Print the current call stack
        System.out.println("Call stack:");
        for (int i = stack.size() - 1; i >= 0; i--) {
            System.out.println(stack.elementAt(i).print());
        }
        return new Tuple<>(Boolean.FALSE, null);
    }
}

