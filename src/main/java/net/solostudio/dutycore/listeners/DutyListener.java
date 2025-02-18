package net.solostudio.dutycore.listeners;

import com.earth2me.essentials.Essentials;
import com.github.Anon8281.universalScheduler.scheduling.tasks.MyScheduledTask;
import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.commands.CommandDuty;
import net.solostudio.dutycore.enums.keys.ConfigKeys;
import net.solostudio.dutycore.events.AdminChatEvent;
import net.solostudio.dutycore.events.DutyJoinEvent;
import net.solostudio.dutycore.events.DutyLeaveEvent;
import net.solostudio.dutycore.hooks.plugins.EssentialsX;
import net.solostudio.dutycore.interfaces.DutyDatabase;
import net.solostudio.dutycore.utils.DutyUtils;
import net.solostudio.dutycore.utils.EventUtils;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerMoveEvent;
import org.bukkit.event.player.PlayerQuitEvent;

import java.util.Objects;

public class DutyListener implements Listener {
    private static MyScheduledTask dutyTask;

    @EventHandler
    public void onDutyJoin(final DutyJoinEvent event) {
        String staff = event.getStaff();
        Player player = Bukkit.getPlayerExact(staff);
        DutyDatabase database = DutyCore.getDatabase();
        assert player != null;

        EventUtils.handleEvent("webhook.duty-join", event);

        if (!player.isOnline()) return;

        dutyTask = DutyCore.getInstance().getScheduler().runTaskTimer(() -> {
            if (!EssentialsX.isAFK(player)) {
                database.updateDutyTime(staff);
                database.updateServedTime(staff);
            }
        }, 20L, 20L);

        DutyUtils.sendActionbar("join", Objects.requireNonNull(Bukkit.getPlayerExact(event.getStaff())));
        DutyUtils.executeEventCommands("join", Objects.requireNonNull(Bukkit.getPlayerExact(staff)));
    }

    @EventHandler
    public void onDutyLeave(final DutyLeaveEvent event) {
        String staff = event.getStaff();

        EventUtils.handleEvent("webhook.duty-leave", event);

        dutyTask.cancel();
        DutyCore.getDatabase().clearServedTime(staff);
        DutyUtils.sendActionbar("leave", Objects.requireNonNull(Bukkit.getPlayerExact(event.getStaff())));
        DutyUtils.executeEventCommands("leave", Objects.requireNonNull(Bukkit.getPlayerExact(staff)));
    }

    @EventHandler
    public void onMove(final PlayerMoveEvent event) {
        Player player = event.getPlayer();

        if (CommandDuty.getFreezeList().contains(player)) event.setCancelled(true);
    }

    @EventHandler
    public void onQuit(final PlayerQuitEvent event) {
        Player player = event.getPlayer();

        if (DutyCore.getDatabase().isInDuty(player.getName())) {
            DutyCore.getDatabase().leaveDuty(player.getName());
            dutyTask.cancel();
            DutyUtils.sendActionbar("leave", player);
            DutyUtils.executeEventCommands("leave", player);
        }
    }

    @EventHandler
    public void onChat(final AdminChatEvent event) {
        EventUtils.handleEvent("webhook.duty-admin-chat", event);
    }
}
