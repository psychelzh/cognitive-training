data_repos = fullfile('n-back', {'logs_pre', 'logs_post'});
data = cell(length(data_repos), 1);
for i_data_repo = 1:length(data_repos)
    data_repo = data_repos{i_data_repo};
    data_files = dir(fullfile(data_repo, '*.mat'));
    data_this_repo = table;
    for i_data_file = 1:length(data_files)
        data_filename = data_files(i_data_file).name;
        data_content = load(fullfile(data_repo, data_filename));
        user_this_file = struct2table(data_content.user);
        data_this_file = table;
        for run = 1:2
            result_name = sprintf('test_run%d', run);
            if ~isfield(data_content, result_name)
                continue
            end
            data_this_run = data_content.(result_name).recordings;
            data_this_run = addvars(data_this_run, ...
                repmat(run, height(data_this_run), 1), ...
                'Before', 1, 'NewVariableNames', 'run');
            data_this_file = cat(1, data_this_file, data_this_run);
        end
        if ~isempty(data_this_file)
            data_this_file = addvars(data_this_file, ...
                repmat(user_this_file.id, height(data_this_file), 1), ...
                repmat(user_this_file.create_time, height(data_this_file), 1), ...
                'Before', 1, 'NewVariableNames', {'id', 'create_time'});
            data_this_repo = cat(1, data_this_repo, data_this_file);
        end
    end
    [~, occasion_string] = fileparts(data_repo);
    occasion = regexp(occasion_string, '(?<=_)\w+', 'match');
    data_this_repo = addvars(data_this_repo, ...
        repmat(occasion, height(data_this_repo), 1), ...
        'After', 1, 'NewVariableNames', 'occasion');
    data{i_data_repo} = data_this_repo;
end
data = cat(1, data{:});
writetable(data, fullfile('n-back', 'results.txt'), 'Delimiter', '\t');
