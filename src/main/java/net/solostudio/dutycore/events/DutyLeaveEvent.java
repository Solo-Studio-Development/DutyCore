package net.solostudio.dutycore.events;

import lombok.Getter;
import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.interfaces.DutyDatabase;
import net.solostudio.dutycore.interfaces.PlaceholderProvider;
import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;

@Getter
public class DutyLeaveEvent extends Event implements PlaceholderProvider {
    private static final HandlerList handlers = new HandlerList();
    private final String staff;
    private final String served;
    private final String time;

    public DutyLeaveEvent(@NotNull String staff, @NotNull String served, @NotNull String time) {
        this.staff = staff;
        this.served = served;
        this.time = time;
    }

    public static HandlerList getHandlerList() {
        return handlers;
    }

    @Override
    public Map<String, String> getPlaceholders() {
        Map<String, String> placeholders = new HashMap<>();
        DutyDatabase database = DutyCore.getDatabase();

        placeholders.put("{player}", staff);
        placeholders.put("{served}", database.getFormattedServedTime(staff));
        placeholders.put("{time}", database.getFormattedDutyTime(staff));

        return placeholders;
    }

    @Override
    public @NotNull HandlerList getHandlers() {
        return handlers;
    }
}
