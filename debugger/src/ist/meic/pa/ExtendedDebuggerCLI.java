package ist.meic.pa;

import ist.meic.pa.command.RetryArgsCommand;

/**
 * This class contains all the extra functionality of the Debugger class.
 */
public class ExtendedDebuggerCLI {

    public ExtendedDebuggerCLI() {
        addExtraCommands();
    }

    private void addExtraCommands() {
        Debugger.getInstance().addCommand("RetryArgs", new RetryArgsCommand());
    }

}
