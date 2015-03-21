package ist.meic.pa;

import ist.meic.pa.command.AbortCommand;
import ist.meic.pa.command.InfoCommand;
import ist.meic.pa.command.ReturnCommand;
import ist.meic.pa.command.ThrowCommand;
import ist.meic.pa.parser.*;

public class DebuggerBootstrap {

    public static void bootstrap(Debugger debugger){
        initParameterParsers(debugger);
        initCommands(debugger);
    }

   private static void initCommands(Debugger debugger) {
       debugger.addCommand("abort", new AbortCommand());
       debugger.addCommand("info", new InfoCommand());
       debugger.addCommand("return", new ReturnCommand());
       debugger.addCommand("throw", new ThrowCommand());
   }

    private static void initParameterParsers(Debugger debugger) {
        debugger.addParameterParser("byte", new ByteParser());
        debugger.addParameterParser("short", new ShortParser());
        debugger.addParameterParser("int", new IntegerParser());
        debugger.addParameterParser("long", new LongParser());
        debugger.addParameterParser("float", new FloatParser());
        debugger.addParameterParser("double", new DoubleParser());
        debugger.addParameterParser("boolean", new ByteParser());
        debugger.addParameterParser("char", new CharParser());
    }
}
