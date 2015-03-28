package ist.meic.pa.parser;

public class ShortParser implements Parser {
    @Override
    public Object parse(String value) {
        return Short.parseShort(value);
    }
}
