package com.pava.shell;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;
import java.util.stream.Collectors;

public class GenericCommand implements Command {
    @Override
    public Object execute(String[] args, Map<String, Object> variables, Object lastResult) {
        String methodName = args[0];
        Object[] methodArgs = args.length == 1 ? new Object[0] : parseArgs(args[1]);
        Class[] methodArgTypes = getTypes(methodArgs);
        try {
            Method method = bestMethod(lastResult.getClass(), methodName, methodArgTypes);
            System.out.println("Trying generic command: " + methodName);

            Object result = null;
            if (methodName.equals("newInstance")) {
                result = method.invoke(lastResult, new Object[] { methodArgs });
            } else {
                result = method.invoke(lastResult, methodArgs);
            }
            print(result);
            return result;
        } catch (NoSuchMethodException | InvocationTargetException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Returns the method that matches the name and the argument types.
     */
    @SuppressWarnings("unchecked")
    private static Method bestMethod(Class clazz, String name, Class[] argTypes) throws NoSuchMethodException {
        if (name.equals("newInstance")) {
            return clazz.getDeclaredMethod(name, Object[].class);
        }

        List<Class[]> argCombinations = getAllArgumentCombinations(argTypes);
        print(argCombinations);

        if (argCombinations.isEmpty()) {
            try {
                return clazz.getDeclaredMethod(name, (Class[]) null);
            } catch (NoSuchMethodException e) {
                // Empty on purpose
            }
        } else {
            for (Class[] arg : argCombinations) {
                try {
                    return clazz.getDeclaredMethod(name, arg);
                } catch (NoSuchMethodException e) {
                    // Empty on purpose
                }
            }
        }
        if (clazz.getSuperclass() == null) {
            throw new NoSuchMethodException();
        } else {
            return bestMethod(clazz.getSuperclass(), name, argTypes);
        }
    }

    /**
     * Returns all the possible combinations of the types. It starts in the derived type and moves upwards until the
     * Object class.
     */
    private static List<Class[]> getAllArgumentCombinations(Class[] args) {
        if (args == null) {
            return null;
        }

        List<Class[]> res = new LinkedList<>();

        for (final Class arg : args) {
            if (res.size() == 0) {
                Class c = arg;
                while (c != null) {
                    res.add(new Class[] { c });
                    c = c.getSuperclass();
                }
            } else {
                res = res.stream()
                        .map(cs -> {
                            List<Class[]> r = new LinkedList<>();
                            Class c = arg;
                            while (c != null) {
                                List<Class> csList = Arrays.asList(cs);
                                csList.add(c);
                                r.add((Class[]) csList.toArray());
                                c = c.getSuperclass();
                            }
                            return r;
                        }).flatMap(r -> r.stream()).collect(Collectors.toList());
            }
        }
        return res;
    }

    /**
     * Gets the types of the objects
     */
    private static Class[] getTypes(Object[] objects) {
        Class[] classes = new Class[objects.length];
        for (int i = 0; i < objects.length; i++) {
            classes[i] = objects[i].getClass();
        }
        return classes;
    }

    /**
     * Parses the arguments from a string, instantiates them, and returns them.
     */
    private static Object[] parseArgs(String text) {
        // Assuming there is only one argument
        try {
            return new Object[] { Integer.parseInt(text) };
        } catch (NumberFormatException e) {}

        try {
            return new Object[] { Double.parseDouble(text) };
        } catch (NumberFormatException e) {}

        // Assuming this is a String
        return new Object[] { text };
    }

    /**
     * Prints the result. In case it's an array, it prints its elements.
     */
    private static void print(Object result) {
        if (result instanceof Object[]) {
            Object[] arr = (Object[]) result;
            for (Object o : arr) {
                System.out.println(o.toString());
            }
        } else {
            System.out.println(result.toString());
        }
    }

    /**
     * Prints all argument combinations
     */
    private static void print(List<Class[]> argCombinations) {
        argCombinations.forEach(arg -> {
            System.out.print("(");
            for (Class c : arg) {
                System.out.print(c.getName() + ", ");
            }
            System.out.println("),");
        });
    }
}
