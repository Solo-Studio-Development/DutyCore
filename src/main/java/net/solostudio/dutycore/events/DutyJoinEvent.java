package net.solostudio.dutycore.events;

import lombok.Getter;
import net.solostudio.dutycore.interfaces.PlaceholderProvider;
import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;

@Getter
public class DutyJoinEvent extends Event implements PlaceholderProvider {
    private static final HandlerList handlers = new HandlerList();
    private final String staff;

    public DutyJoinEvent(@NotNull String staff) {
        this.staff = staff;
    }

    public static HandlerList getHandlerList() {
        return handlers;
    }

    @Override
    public Map<String, String> getPlaceholders() {
        Map<String, String> placeholders = new HashMap<>();

        placeholders.put("{player}", staff);

        return placeholders;
    }

    @Override
    public @NotNull HandlerList getHandlers() {
        return handlers;
    }
}
