package ist.meic.pa.command;

import ist.meic.pa.Debugger;
import ist.meic.pa.MethodCallEntry;

import java.lang.reflect.Field;
import java.util.Stack;

public class SetCommand implements Command {
    @Override
    public Object execute(Stack<MethodCallEntry> stack, Object calledObject, String[] args) throws Throwable {
        Object parsedValue = Debugger.getParameterParser(args[1]);
        Field field = calledObject.getClass().getDeclaredField(args[0]);
        field.setAccessible(true);
        field.set(calledObject,parsedValue);
        return calledObject;
    }
}
