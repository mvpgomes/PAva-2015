package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;

import java.lang.reflect.Field;
import java.util.Stack;

public class GetCommand extends Command {
    @Override
    public Object execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) {
        Object instance = stack.peek().getInstance();
        try {
            Field field = instance.getClass().getDeclaredField(args[0]);
            field.setAccessible(true);

            // Print instance.field
            System.out.println(field.get(instance));

            return null;
        } catch (NoSuchFieldException | IllegalAccessException e) {
          throw new RuntimeException(e);
        }
    }
}
