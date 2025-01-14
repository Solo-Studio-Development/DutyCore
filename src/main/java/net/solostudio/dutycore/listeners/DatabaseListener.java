package net.solostudio.dutycore.listeners;

import net.solostudio.dutycore.DutyCore;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerJoinEvent;

public class DatabaseListener implements Listener {
    @EventHandler
    public void onJoin(final PlayerJoinEvent event) {
        Player player = event.getPlayer();

        if (player.hasPermission("dutycore.staff")) DutyCore.getDatabase().createPlayer(player.getName());
        if (DutyCore.getDatabase().exists(player.getName()) && !player.hasPermission("dutycore.staff")) DutyCore.getDatabase().deletePlayer(player.getName());
    }
}
