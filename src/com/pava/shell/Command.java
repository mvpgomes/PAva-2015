package com.pava.shell;

import java.util.List;
import java.util.Map;

/**
 * Created by Marcus on 3/4/15.
 */
public interface Command {
    Object execute(String[] args, Map<String, Object> variables, Object lastClass);
}
