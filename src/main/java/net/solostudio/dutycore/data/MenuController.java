package net.solostudio.dutycore.data;

import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public record MenuController(@NotNull Player owner) {
    private static final Map<Player, MenuController> menuMap = new ConcurrentHashMap<>();

    public static MenuController getMenuUtils(@NotNull Player player) {
        return menuMap.computeIfAbsent(player, MenuController::new);
    }
}
