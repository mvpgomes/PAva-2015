package ist.meic.pa;

import ist.meic.pa.command.AbortCommand;
import ist.meic.pa.command.InfoCommand;
import ist.meic.pa.command.ReturnCommand;
import ist.meic.pa.command.ThrowCommand;
import ist.meic.pa.parser.*;

public class DebuggerBootstrap {

    public static void bootstrap(){
        initParameterParsers();
        initCommands();
    }

   private static void initCommands() {
       Debugger.addCommand("abort", new AbortCommand());
       Debugger.addCommand("info", new InfoCommand());
       Debugger.addCommand("return", new ReturnCommand());
       Debugger.addCommand("throw", new ThrowCommand());
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
