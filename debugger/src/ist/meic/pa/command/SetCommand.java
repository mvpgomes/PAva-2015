package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;
import ist.meic.pa.Tuple;

import java.lang.reflect.Field;
import java.util.Stack;

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
