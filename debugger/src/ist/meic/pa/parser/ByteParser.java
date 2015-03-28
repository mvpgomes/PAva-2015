package ist.meic.pa.parser;

public class ByteParser implements Parser {
    @Override
    public Object parse(String value) {
        return Byte.parseByte(value);
    }
}
