package net.solostudio.dutycore.enums.keys;

import lombok.Getter;
import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.processor.MessageProcessor;
import org.jetbrains.annotations.NotNull;

import java.util.List;

@Getter
public enum MessageKeys {
    RELOAD("messages.reload"),

    INVALID_PLAYER("messages.invalid-player"),
    PLAYER_REQUIRED("messages.player-required"),
    MISSING_ARGUMENT("messages.missing-argument"),
    NO_PERMISSION("messages.no-permission"),

    REQUIRES_DUTY("messages.requires-duty"),
    REQUIRES_DUTY_OTHER("messages.requires-duty-other"),
    DUTY_JOIN("messages.duty-join"),
    DUTY_LEAVE("messages.duty-leave"),
    ALREADY_DUTY("messages.already-duty"),
    ALREADY_DUTY_OTHER("messages.already-duty-other"),

    FREEZE_ON_SENDER("messages.freeze-on-sender"),
    FREEZE_ON_TARGET("messages.freeze-on-target"),
    FREEZE_OFF_TARGET("messages.freeze-off-target"),
    FREEZE_OFF_SENDER("messages.freeze-off-sender"),

    STAFF_RANK("messages.success-staff-rank"),
    PLAYER_RANK("messages.success-player-rank"),

    STATS_RESET("messages.success-stats-reset"),

    SUCCESS_BADGE("messages.success-badge"),
    SUCCESS_FORCE_LEAVE("messages.success-forceleave"),
    SUCCESS_FORCE_JOIN("messages.success-forcejoin"),

    STATS("messages.stats");

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
