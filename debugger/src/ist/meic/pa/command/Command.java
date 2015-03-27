package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;
import ist.meic.pa.Tuple;
import ist.meic.pa.parser.*;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

/**
 * This interface serves as an example of the command pattern.
 */
public abstract class Command {
    private final Map<String, Parser> parameterParser = new HashMap<String, Parser>() {{
        put("byte", new ByteParser());
        put("short", new ShortParser());
        put("int", new IntegerParser());
        put("long", new LongParser());
        put("float", new FloatParser());
        put("double", new DoubleParser());
        put("boolean", new ByteParser());
        put("char", new CharParser());
        put("String", new StringParser());
    }};

    public Parser getParameterParser(String s) {
        return parameterParser.get(s);
    }

    /**
     * The method to be implemented by the commands.
     *
     * @param stack the method call stack of the program when the repl was invoked.
     * @param args a list of arguments given by the user.
     * @param t the exception that originated the repl.
     * @return a tuple (finalCommand, returnValue). when finalCommand is true, the repl should exit and return the
     * returnValue.
     * @throws Throwable it is also possible for commands to generate an exception. This can be used to roll back in the
     * method call stack.
     */
    public abstract Tuple<Boolean, Object> execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) throws Throwable;
}
