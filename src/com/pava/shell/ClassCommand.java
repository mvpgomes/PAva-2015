package com.pava.shell;

import java.util.List;
import java.util.Map;

/**
 * Created by Marcus on 3/4/15.
 */
public class ClassCommand implements Command {
    @Override
    public Object execute(String[] args, Map<String, Object> variables, Object lastResult) {
        try {
            return Class.forName(args[0]);
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
        return null;
    }
}
