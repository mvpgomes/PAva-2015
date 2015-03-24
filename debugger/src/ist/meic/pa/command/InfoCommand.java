package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;

import java.lang.reflect.Field;
import java.util.Stack;

public class InfoCommand implements Command {
    @Override
    public Object execute(Stack<MethodCallEntry> stack, Object calledObject, String[] args) throws Throwable {
        System.out.print("Called Object:");
        // Print the called object
        System.out.println(calledObject.toString());
        // Print information about the object
        Field[] fields = calledObject.getClass().getDeclaredFields();
        for(Field f : fields){
            System.out.println(f.toString());
        }
        // Print the contents of the stack
        System.out.println("Call stack:");
        for(int i=stack.size() - 1; i >= 0; i--) {
            System.out.println(stack.elementAt(i).toString());
        }
        return calledObject;
    }
}

