package net.solostudio.dutycore.utils;

import lombok.experimental.UtilityClass;
import net.luckperms.api.LuckPerms;
import net.luckperms.api.model.user.User;
import net.luckperms.api.query.QueryOptions;
import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.config.Config;
import net.solostudio.dutycore.processor.MessageProcessor;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;

import java.util.List;

@UtilityClass
@SuppressWarnings({"deprecation", "unchecked"})
public class DutyUtils {
    public void sendActionbar(@NotNull String actionType, @NotNull Player player) {
        Config config = DutyCore.getInstance().getConfiguration();

        String path = actionType + "-actionbar";
        boolean isEnabled = config.getBoolean(path + ".enabled");
        String messageTemplate = config.getString(path + ".message");
        boolean sendToInDuty = config.getBoolean(path + ".send-it-to-in-duty");
        boolean sendItToEveryone = config.getBoolean(path + ".send-it-to-everyone");

        if (!isEnabled) return;

        String formattedMessage = MessageProcessor.process(messageTemplate.replace("{player}", player.getName()));


        Bukkit.getOnlinePlayers().stream()
                .filter(onlinePlayer -> {
                    if (sendItToEveryone) return true;

                    boolean isInDuty = DutyCore.getDatabase().isInDuty(onlinePlayer.getName());
                    boolean hasStaffPermission = onlinePlayer.hasPermission("dutycore.staff");

                    return (sendToInDuty && isInDuty) || (!sendToInDuty && hasStaffPermission);
                })
                .forEach(onlinePlayer -> onlinePlayer.sendActionBar(formattedMessage));
    }

    public void executeEventCommands(@NotNull String eventType, @NotNull Player player) {
        var commandsSection = DutyCore.getInstance().getConfiguration().getSection(eventType + "-commands");

        if (commandsSection == null) return;

        LuckPerms luckPerms = DutyCore.getInstance().getProvider().getProvider();
        User user = luckPerms.getUserManager().getUser(player.getUniqueId());

        if (user == null) return;

        QueryOptions queryOptions = luckPerms.getContextManager().getQueryOptions(player);

        if (commandsSection.contains("every")) {
            List<String> everyCommands = commandsSection.getStringList("every");
            everyCommands.forEach(command -> runCommand(player, command));
        }

        commandsSection.getKeys(false).stream()
                .filter(group -> !group.equals("every"))
                .sorted((group1, group2) -> {
                    var group1Data = luckPerms.getGroupManager().getGroup(group1);
                    var group2Data = luckPerms.getGroupManager().getGroup(group2);

                    if (group1Data == null || group2Data == null) return 0;
                    return Integer.compare(group2Data.getWeight().orElse(0), group1Data.getWeight().orElse(0));
                })
                .filter(group -> user.getCachedData().getPermissionData(queryOptions).checkPermission("group." + group).asBoolean())
                .findFirst()
                .ifPresent(group -> {
                    List<String> groupCommands = commandsSection.getStringList(group);
                    groupCommands.forEach(command -> runCommand(player, command));
                });
    }

    private void runCommand(@NotNull Player player, @NotNull String command) {
        DutyCore.getInstance().getScheduler().runTask(() -> Bukkit.dispatchCommand(Bukkit.getConsoleSender(), command.replace("%player%", player.getName())));
    }

}
