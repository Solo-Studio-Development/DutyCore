package net.solostudio.dutycore.events;

import lombok.Getter;
import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

@Getter
public class DutyJoinEvent extends Event {
    private static final HandlerList handlers = new HandlerList();
    private final String staff;

    public DutyJoinEvent(@NotNull String staff) {
        this.staff = staff;
    }

    public static HandlerList getHandlerList() {
        return handlers;
    }

    @Override
    public @NotNull HandlerList getHandlers() {
        return handlers;
    }
}
