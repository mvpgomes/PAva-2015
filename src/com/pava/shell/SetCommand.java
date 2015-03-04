package com.pava.shell;

import java.util.Map;

public class SetCommand implements Command {
    @Override
    public Object execute(String[] args, Map<String, Object> variables, Object lastResult) {
        variables.put(args[0], lastResult);
        return lastResult;
    }
}
