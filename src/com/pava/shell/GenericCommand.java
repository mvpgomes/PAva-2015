package com.pava.shell;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Map;
import java.util.stream.Stream;

public class GenericCommand implements Command {
    // DEBUG Class.forName("java.lang.String").getDeclaredConstructors()[12].newInstance("ola")
    @Override
    public Object execute(String[] args, Map<String, Object> variables, Object lastResult) {
        String methodName = args[0];
        Object[] methodArgs = args.length == 1 ? null : parseArgs(args[1]);
        try {
            Method method = bestMethod(lastResult.getClass(), methodName, methodArgs);
            System.out.println("Trying generic command: " + methodName);

            Object result = method.invoke(lastResult, methodArgs);
            print(result);
            return result;
        } catch (NoSuchMethodException | InvocationTargetException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    private Method bestMethod(Class clazz, String name, Object[] args) throws NoSuchMethodException {
        Stream<Method> methods = Arrays.asList(clazz.getDeclaredMethods()).stream()
                .filter(m -> m.getName().equals(name));
        return methods.findFirst().orElseThrow(NoSuchMethodException::new);
    }

    private Class[] getTypes(Object[] objects) {
        Class[] classes = new Class[objects.length];
        for (int i = 0; i < objects.length; i++) {
            Class c = objects[i].getClass();
            classes[i] = c;
        }
        return classes;
    }

    private Object[] parseArgs(String args) {
        // Assuming there is only one argument
        try {
            return new Object[] { Integer.parseInt(args) };
        } catch (NumberFormatException e) {}

        try {
            return new Object[] { Double.parseDouble(args) };
        } catch (NumberFormatException e) {}

        // Assuming this is a String
        return new Object[] { args };
    }

    private void print(Object result) {
        if (result instanceof Object[]) {
            Object[] arr = (Object[]) result;
            for (Object o : arr) {
                System.out.println(o.toString());
            }
        } else {
            System.out.println(result.toString());
        }
    }
}
