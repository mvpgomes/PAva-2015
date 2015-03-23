package ist.meic.pa;

import ist.meic.pa.command.*;
import ist.meic.pa.parser.*;

public class DebuggerBootstrap {

    public static void bootstrap(){
        initParameterParsers();
        initCommands();
    }

   private static void initCommands() {
       Debugger.addCommand("Abort", new AbortCommand());
       Debugger.addCommand("Info", new InfoCommand());
       Debugger.addCommand("Return", new ReturnCommand());
       Debugger.addCommand("Throw", new ThrowCommand());
       Debugger.addCommand("Get", new GetCommand());
       Debugger.addCommand("Set", new SetCommand());
   }

    private static void initParameterParsers() {
        Debugger.addParameterParser("byte", new ByteParser());
        Debugger.addParameterParser("short", new ShortParser());
        Debugger.addParameterParser("int", new IntegerParser());
        Debugger.addParameterParser("long", new LongParser());
        Debugger.addParameterParser("float", new FloatParser());
        Debugger.addParameterParser("double", new DoubleParser());
        Debugger.addParameterParser("boolean", new ByteParser());
        Debugger.addParameterParser("char", new CharParser());
        Debugger.addParameterParser("String", new StringParser());
    }
}
