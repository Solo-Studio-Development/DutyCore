package net.solostudio.dutycore.enums.keys;

import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.processor.MessageProcessor;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public enum ConfigKeys {
    LANGUAGE("language"),
    DATABASE("database.type"),
    ALIASES("aliases"),

    TRUE("true"),
    FALSE("false"),

    DUTY_CHAT_FORMAT("duty-chat-format");

    private final String path;

    ConfigKeys(@NotNull final String path) {
        this.path = path;
    }

    public String getString() {
        return MessageProcessor.process(DutyCore.getInstance().getConfiguration().getString(path));
    }

    public boolean getBoolean() {
        return DutyCore.getInstance().getConfiguration().getBoolean(path);
    }

    public int getInt() {
        return DutyCore.getInstance().getConfiguration().getInt(path);
    }

    public List<String> getList() {
        return DutyCore.getInstance().getConfiguration().getList(path);
    }
}
