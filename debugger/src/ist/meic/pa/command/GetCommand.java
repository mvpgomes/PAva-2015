package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;
import ist.meic.pa.Tuple;

import java.lang.reflect.Field;
import java.util.Stack;

/**
 * The get command prints the value of a given variable that belongs to the last object in the call stack.
 * This command expects an argument, args[0], to be the name of the variable.
 */
public class GetCommand implements Command {
    @Override
    public Tuple<Boolean, Object> execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) {
        final MethodCallEntry entry = stack.peek();
        try {
            Class instanceClass = entry.getInstanceClass();
            Field field = instanceClass.getDeclaredField(args[0]);
            field.setAccessible(true);

            // Print instance.field
            Object instance = entry.getInstance();
            System.out.println(field.get(instance));

            return new Tuple<>(Boolean.FALSE, null);
        } catch (NoSuchFieldException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }
}
