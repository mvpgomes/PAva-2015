package ist.meic.pa;

import ist.meic.pa.parser.*;

import java.util.HashMap;
import java.util.Map;

/**
 * This class is used to convert any string to a primitive type.
 */
public class GenericParser {
    private static Map<Class, Parser> parameterParser = new HashMap<Class, Parser>() {{
        put(byte.class, new ByteParser());
        put(short.class, new ShortParser());
        put(int.class, new IntegerParser());
        put(long.class, new LongParser());
        put(float.class, new FloatParser());
        put(double.class, new DoubleParser());
        put(boolean.class, new ByteParser());
        put(char.class, new CharParser());
        put(String.class, new StringParser());
    }};

    public static Object parse(Class returnType, String arg){
        return parameterParser.get(returnType).parse(arg);
    }
}
