package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;
import ist.meic.pa.parser.*;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

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

    public Map<String, Parser> getParameterParser() {
        return parameterParser;
    }

    public abstract Object execute(Stack<MethodCallEntry> stack, Object calledObject, String[] args, Throwable t) throws Throwable;
}
