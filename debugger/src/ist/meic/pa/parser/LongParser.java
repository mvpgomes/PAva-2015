package ist.meic.pa.parser;

public class LongParser extends Parser {
    @Override
    public Object parse(String value) {
        return Long.parseLong(value);
    }
}
