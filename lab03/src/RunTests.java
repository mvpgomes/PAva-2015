import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class RunTests {
    private static Method[] filterSetups(Class c) {
        Method[] methods = c.getDeclaredMethods();
        List<Method> setupMethods = Arrays.asList(methods).stream()
                .filter(m -> m.isAnnotationPresent(Setup.class) &&
                        Modifier.isStatic(m.getModifiers()) &&
                        m.getReturnType().getName().equals("void") &&
                        m.getParameterCount() == 0)
                .collect(Collectors.toList());
        return setupMethods.toArray(new Method[setupMethods.size()]);
    }

    private static Method[] filterTests(Class c) {
        Method[] methods = c.getDeclaredMethods();
        List<Method> testMethods = Arrays.asList(methods).stream()
                .filter(m -> m.isAnnotationPresent(Test.class) &&
                        Modifier.isStatic(m.getModifiers()) &&
                        m.getReturnType().getName().equals("void") &&
                        m.getParameterCount() == 0)
                .collect(Collectors.toList());
        return testMethods.toArray(new Method[testMethods.size()]);
    }

    private static void runSetup(Method m) {
        if (Modifier.isPrivate(m.getModifiers())) {
            m.setAccessible(true);
            try {
                m.invoke(null);
            } catch (IllegalAccessException | InvocationTargetException e) {
                throw new RuntimeException(e);
            }
            m.setAccessible(false);
        } else {
            try {
                m.invoke(null);
            } catch (IllegalAccessException | InvocationTargetException e) {
                throw new RuntimeException(e);
            }
        }
    }

    private static Method findSetupMethod(Class c, String methodName) throws NoSuchMethodException {
        try {
            return c.getDeclaredMethod(methodName);
        } catch (NoSuchMethodException e) {
            if (c.getSuperclass() != null) {
                return findSetupMethod(c.getSuperclass(), methodName);
            } else {
                throw new NoSuchMethodException();
            }
        }
    }

    private static void runAllSetupMethods(Class c) {
        if (c == null) {
            return;
        }

        // run parent setup methods
        runAllSetupMethods(c.getSuperclass());

        // run this class setup methods
        Method[] setupMethods = filterSetups(c);
        for (Method sm : setupMethods) {
            runSetup(sm);
        }
    }

    private static void runTest(Class owner, Method m) {
        Test annotation = m.getAnnotation(Test.class);

        // run setup tests
        for (String setupName : annotation.value()) {
            try {
                switch (setupName) {
                    case "*":
                        runAllSetupMethods(owner);
                        break;

                    default:
                        Method setupTest = findSetupMethod(owner, setupName);
                        runSetup(setupTest);
                        break;
                }
            } catch (NoSuchMethodException e) {
                throw new RuntimeException(e);
            }
        }

        if (Modifier.isPrivate(m.getModifiers())) {
            m.setAccessible(true);
            try {
                m.invoke(null);
            } catch (IllegalAccessException | InvocationTargetException e) {
                throw new RuntimeException(e);
            }
            m.setAccessible(false);
        } else {
            try {
                m.invoke(null);
            } catch (IllegalAccessException | InvocationTargetException e) {
                throw new RuntimeException(e);
            }
        }
    }

    private static Score runTestMethods(Class c, Score score) {
        Method[] testMethods = filterTests(c);
        for (Method m : testMethods) {
            try {
                runTest(c, m);
                score.passedTest();
                System.out.println("Test " + m.toString() + " OK!");
            } catch (RuntimeException e) {
                score.failedTest();
                System.out.println("Test " + m.toString() + " failed");
            }
        }
        return score;
    }

    private static void testClass(Class c, Score score) {
        if (c == null) {
            return;
        }

        // tests parent class
        testClass(c.getSuperclass(), score);

        // run the tests in this class
        runTestMethods(c, score);
    }

    public static void main(String[] args) {
        if (args.length < 1) {
            System.err.println("Not enough arguments");
            System.exit(1);
        }

        String className = args[0];

        try {
            Class c = Class.forName(className);
            Score score = new Score();
            testClass(c, score);
            System.out.println("Passed: " + score.getPassed() + ", Failed: " + score.getFailed());
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }
}
