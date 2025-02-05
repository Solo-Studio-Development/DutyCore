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

    DUTY_CHAT_FORMAT("duty-chat-format"),
    DUTY_JOIN_COMMANDS("join-commands"),
    DUTY_LEAVE_COMMANDS("leave-commands"),

    NAVIGATION_MENU_TITLE("navigation-menu.title"),
    NAVIGATION_MENU_SIZE("navigation-menu.size"),

    STAFF_MENU_TITLE("staff-menu.title"),
    STAFF_MENU_SIZE("staff-menu.size"),
    STAFF_MENU_TICK("staff-menu.tick");

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
