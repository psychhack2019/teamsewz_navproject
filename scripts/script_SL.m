%%
% sessions: 1 - train, 2 - test
% delay: 1 - short, 2 - long
% reward_type: 0 - NoReward, 1 - F(food), 2 - M(money), 3 - W(water)
% trial_type = 0 - 3D, 1 - 2D


%%
% block 1 - ???
% block 2 - familiarization
% block 3 ~ 9 - blocked training
% block 10 - interleaved training


%% video
v_obj = VideoWriter('test');
v_obj.FrameRate = 10;
open(v_obj);
figure('Color',[1 1 1]); hold on
xlim([-15,15])
ylim([-15,15]);
set(gca,'xticklabel',[])
set(gca,'yticklabel',[])
grid on
box on
for t = 1:length(trial)
    current_trial = trial{t};
    switch current_trial(1,7)
        case 1
            color_str = 'r';
        case 2
            color_str = 'g';
        case 3
            color_str = 'b';
    end
    f1 = scatter(current_trial(1,3),current_trial(1,4),100,color_str,'filled');hold on
    f1.MarkerFaceAlpha = 0.2;
    xlim([-15,15])
    ylim([-15,15]);
    set(gca,'xticklabel',[])
    set(gca,'yticklabel',[])
    grid on
    box on
    for i = 1:10:size(current_trial,1)
        
        plot(current_trial(1:i,1),current_trial(1:i,2),'k','LineWidth',8);
        frame = getframe(gcf);
        writeVideo(v_obj,frame);
    end
    hold off
end
close(v_obj)


%%
all_filenames = {};
for i = 1:length(sub_id)
    if sub_id(i) < 10
        all_filenames{i} = ['S0',num2str(sub_id(i))];
    else
        all_filenames{i} = ['S',num2str(sub_id(i))];
    end
end

%% after 5 seconds
for i = 1:length(trial_data_test)
    i
    current_sub = trial_data_test{i};
    for j = 1:length(current_sub)
        current_blk = current_sub{j};
        for k = 1:length(current_blk)
            current_trial = current_blk{k};
            %start_ind = find(((current_trial(:,1)~=0) & (current_trial(:,2)~=0)),1,'first');
            start_ind = find(current_trial(:,10)>=5,1,'first');
            current_trial = current_trial(start_ind:end,:);
            trial_data_test{i}{j}{k} = current_trial;
        end
    end
end

%%
%sub_ind = 4;
for s = 41:length(sub_id)
    s
    sub_ind = s;
    temp_sub = trial_data_test_5sec{sub_ind};
    temp_trial = temp_sub{3};
    max_step = 0;
    for i = 1:length(temp_trial)
        if size(temp_trial{i},1) > max_step
            max_step = size(temp_trial{i},1);
        end
    end
    
    train_temp_sub = trial_data_train{sub_ind};
    train_temp_sub = train_temp_sub(3:end);
    reward_XY_F = [];
    reward_XY_M = [];
    reward_XY_W = [];
    for i = 1:length(train_temp_sub)
        temp = train_temp_sub{i};
        for j = 1:length(temp)
            temp_trial_train = temp{j};
            switch temp_trial_train(1,7)
                case 1
                    reward_XY_F = [reward_XY_F;temp_trial_train(1,3:4)];
                case 2
                    reward_XY_M = [reward_XY_M;temp_trial_train(1,3:4)];
                case 3
                    reward_XY_W = [reward_XY_W;temp_trial_train(1,3:4)];
            end
        end
    end
    
    
    filename = all_filenames{sub_ind};
    v_obj = VideoWriter(filename);
    v_obj.FrameRate = 10;
    open(v_obj);
    figure('Color',[1 1 1]); hold on
    xlim([-15,15])
    ylim([-15,15]);
    set(gca,'xticklabel',[])
    set(gca,'yticklabel',[])
    grid on
    box on
    
    s1 = scatter(reward_XY_F(:,1), reward_XY_F(:,2),'r','filled');
    s1.MarkerFaceAlpha = 0.2;
    s2 = scatter(reward_XY_M(:,1), reward_XY_M(:,2),'g','filled');
    s2.MarkerFaceAlpha = 0.2;
    s3 = scatter(reward_XY_W(:,1), reward_XY_W(:,2),'b','filled');
    s3.MarkerFaceAlpha = 0.2;
    
    
    
    for i = 1:10:max_step
        
        for j = 1:length(temp_trial)
            current_trial = temp_trial{j};
            if i <= size(current_trial,1)
                
                switch current_trial(1,7)
                    case 1
                        plot(current_trial(1:i,1),current_trial(1:i,2),'r');
                    case 2
                        plot(current_trial(1:i,1),current_trial(1:i,2),'g');
                    case 3
                        plot(current_trial(1:i,1),current_trial(1:i,2),'b');
                end
                %             lgd=legend([s1,s2,s3],'Food','Money','Reward');
                %             lgd.FontSize=15;
            end
        end
        frame = getframe(gcf);
        writeVideo(v_obj,frame);
    end
    hold off
    close(v_obj);
end
%%
trial_data_test = cell(length(sub_id),1);
for s = 1:length(sub_id)
    s
    sub = data_test(s);
    temp_data = cell(length(unique(sub.block)),1);
    % 1 block per cell
    for i = 1:length(temp_data)
%     for i = 3
        blk_ind = sub.block == i;
        blk_X = sub.X(blk_ind);
        blk_Y = sub.Y(blk_ind);
        blk_reward_X = sub.reward_X(blk_ind);
        blk_reward_Y = sub.reward_Y(blk_ind);
        blk_rotation = sub.rotation(blk_ind);
        blk_movement = sub.movement(blk_ind);
        blk_reward_type = sub.reward_type(blk_ind);
        blk_reward_found = sub.reward_found(blk_ind);
        blk_trials = sub.trial(blk_ind);
        blk_visible = sub.visible(blk_ind);
        blk_time = sub.time(blk_ind);
        num_trial = length(unique(blk_trials));
        temp = cell(num_trial,1);
        % 1 trial per cell
        % columns: X, Y, reward X, reward Y, rotation, movement,
        % reward type, reward found, visible, time
        for j = 1:num_trial
            trl_ind = blk_trials==j;
            temp{j} = [blk_X(trl_ind),blk_Y(trl_ind),...
                blk_reward_X(trl_ind),blk_reward_Y(trl_ind),...
                blk_rotation(trl_ind),blk_movement(trl_ind),...
                blk_reward_type(trl_ind),blk_reward_found(trl_ind),...
                blk_visible(trl_ind),blk_time(trl_ind)];
        end
        temp_data{i} = temp;
    end
    trial_data_test{s} = temp_data;
end
%%
sub_id_new = ones(size(sub_id));
for i = 1:length(sub_id)
    sub_id_new(i) = str2num(sub_id{i});
end

%%
session_new = ones(size(data,1),1);
ind = ismember(session,'Test');
session_new(ind) = 2;

%%
delay = data.Delay;
delay_new = ones(size(data,1),1);
ind = ismember(delay,'Long');
delay_new(ind) = 2;

%%
reward_type = data.RewardType;
reward_type_new = zeros(size(data,1),1);
ind1 = ismember(reward_type,'F');
ind2 = ismember(reward_type,'M');
ind3 = ismember(reward_type,'W');
reward_type_new(ind1) = 1;
reward_type_new(ind2) = 2;
reward_type_new(ind3) = 3;
%%
data_new = struct();
for i = 1:length(sub_id)
    i
    ind = data.sub_id == sub_id(i) & data.session == 2;
    data_new(i).session = data.session(ind);
    data_new(i).delay = data.delay(ind);
    data_new(i).reward_type = data.reward_type(ind);
    data_new(i).reward_found = data.rewar_found(ind);
    data_new(i).environment = data.environment(ind);
    data_new(i).block = data.block(ind);
    data_new(i).trial = data.trial(ind);
    data_new(i).trial_type = data.trial_type(ind);
    data_new(i).visible = data.visible(ind);
    data_new(i).time = data.time(ind);
    data_new(i).X = data.X(ind);
    data_new(i).Y = data.Y(ind);
    data_new(i).reward_X = data.reward_X(ind);
    data_new(i).reward_Y = data.reward_Y(ind);
    data_new(i).rotation = data.rotation(ind);
    data_new(i).movement = data.movement(ind);
end




