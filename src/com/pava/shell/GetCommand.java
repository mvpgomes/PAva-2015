package com.pava.shell;

import java.util.Map;

/**
 * Created by Marcus on 3/4/15.
 */
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
