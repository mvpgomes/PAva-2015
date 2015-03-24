package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;

import java.lang.reflect.Field;
import java.util.Stack;

public class GetCommand implements Command {
    @Override
    public Object execute(Stack<MethodCallEntry> stack, Object calledObject, String[] args) throws Throwable {
        Field field = calledObject.getClass().getDeclaredField(args[0]);
        field.setAccessible(true);
        System.out.println(field.get(calledObject));
        return calledObject;
    }
}
