package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;

import java.lang.reflect.Field;
import java.util.Stack;

public class SetCommand extends Command {
    @Override
    public Object execute(Stack<MethodCallEntry> stack, Object calledObject, String[] args, Throwable t) throws Throwable {
        Field field = calledObject.getClass().getDeclaredField(args[0]);
        field.setAccessible(true);
        Object parsedValue = getParameterParser().get(field.getType().getSimpleName()).parse(args[1]);
        field.set(calledObject, parsedValue);
        return calledObject;
    }
}
