package net.solostudio.dutycore.enums.keys;

import lombok.Getter;
import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.processor.MessageProcessor;
import org.jetbrains.annotations.NotNull;

import java.util.List;

@Getter
public enum MessageKeys {
    INVALID_PLAYER("messages.invalid-player"),
    PLAYER_REQUIRED("messages.player-required"),
    MISSING_ARGUMENT("messages.missing-argument"),
    NO_PERMISSION("messages.no-permission");

    private final String path;

    MessageKeys(@NotNull final String path) {
        this.path = path;
    }

    public String getMessage() {
        return MessageProcessor.process(DutyCore.getInstance().getLanguage().getString(path))
                .replace("%prefix%", MessageProcessor.process(DutyCore.getInstance().getLanguage().getString("prefix")));
    }

    public List<String> getMessages() {
        return DutyCore.getInstance().getLanguage().getList(path)
                .stream()
                .map(MessageProcessor::process)
                .toList();
    }
}
