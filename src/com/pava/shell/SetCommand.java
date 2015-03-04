package com.pava.shell;

import java.lang.reflect.InvocationTargetException;
import java.util.Map;

public class SetCommand implements Command {
    @Override
    public Object execute(String[] args, Map<String, Object> variables, Object lastResult) {
        variables.put(args[0], lastResult);
        System.out.println("Saved name for object of type: class " + lastResult.getClass().getName());
        try {
            // the underlying class in this class object
            String className = (String) lastResult.getClass().getMethod("toString").invoke(lastResult);
            System.out.println(className);
        } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
            throw new RuntimeException(e);
        }
        return lastResult;
    }
}
