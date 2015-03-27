package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;
import ist.meic.pa.Tuple;

import java.lang.reflect.Field;
import java.util.Stack;

/**
 * The set command replaces the value of a given variable that belongs to the last object in the call stack with a new
 * value. This command expects two arguments, args[0] and args[1], where args[0] is the name of the variable of the
 * object and args[1] is the new value for that variable.
 */
public class SetCommand extends Command {
    @Override
    public Tuple<Boolean, Object> execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) {
        try {
            final Object instance = stack.peek().getInstance();
            Field field = instance.getClass().getDeclaredField(args[0]);
            field.setAccessible(true);

            String fieldType = field.getType().getSimpleName();
            Object parsedValue = getParameterParser(fieldType).parse(args[1]);
            field.set(instance, parsedValue);

            return new Tuple<>(Boolean.FALSE, null);
        } catch (NoSuchFieldException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }
}
