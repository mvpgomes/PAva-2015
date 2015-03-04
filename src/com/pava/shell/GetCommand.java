package com.pava.shell;

import java.util.Map;

public class GetCommand implements Command {
    @Override
    public Object execute(String[] args, Map<String, Object> variables, Object lastResult) {
        if (variables.containsKey(args[0])) {
            return variables.get(args[0]);
        } else {
            return null;
        }
    }
}
