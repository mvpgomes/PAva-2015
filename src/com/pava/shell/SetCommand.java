package com.pava.shell;

import java.util.List;
import java.util.Map;

/**
 * Created by Marcus on 3/4/15.
 */
public class SetCommand implements Command {
    @Override
    public Object execute(String[] args, Map<String, Object> variables, Object lastResult) {
        variables.put(args[0], lastResult);
        return lastResult;
    }
}
